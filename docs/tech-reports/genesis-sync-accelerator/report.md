# Genesis Sync Accelerator: Technical Design Report

## 1. Introduction

### 1.1 Problem Statement
Ouroboros Genesis allows nodes to sync the historical Cardano chain trustlessly. However, this currently requires fetching all historical blocks from stake pool relays. This imposes:
1.  **Excessive Load:** Stake pool relays must serve terabytes of historical data to new syncing nodes.
2.  **Subpar Performance:** Nodes in geographically remote locations may struggle to find nearby high-bandwidth peers, leading to slow sync times.

### 1.2 Proposed Solution
The **Genesis Sync Accelerator (GSA)** is a tool designed to act as a bridge between low-cost, centralized Content Delivery Networks (CDNs) and the decentralized Ouroboros network.

It functions as a proxy server (extending `immdb-server`) that:
1.  Downloads historical chain data (chunks) from a specified HTTP(S) source (CDN).
2.  Serves this data to a syncing node using standard Ouroboros Node-to-Node protocols (`ChainSync`, `BlockFetch`).

This approach offloads bandwidth from the P2P network to commodity object storage (e.g., S3, Cloudflare R2) while maintaining the trustless properties of Ouroboros Genesis.

---

## 2. Architecture Overview

### 2.1 High-Level Data Flow

```
[ Syncing Node ]  <-- (Ouroboros N2N) -->  [ Genesis Sync Accelerator ]  <-- (HTTPS) -->  [ CDN / Object Storage ]
      |                                           (immdb-server)                                  |
      |                                                 |                                         |
      +--- (Verifies Chain)                             +--- (OnDemand Iterator)                  +--- (Stores .dat/.pri/.sec files)
```

1.  **Syncing Node:** Connects to the Accelerator as if it were a normal peer.
2.  **Accelerator:** Intercepts block requests (`stream_`).
3.  **Local Check:** Checks if the underlying `ImmutableDB` already knows about the requested blocks.
4.  **CDN Fetch:** If unknown, downloads the relevant chunk files from the CDN.
5.  **Bypass & Read Directly:** Instead of attempting to force the monolithic `ImmutableDB` to reload its state (which is rigid and startup-optimized), the Accelerator creates a **Raw Chunk Iterator** managed by the `OnDemand` module. This iterator reads downloaded files directly from disk and streams them to the client, effectively bypassing the main database's in-memory index.

---

## 3. Technical Design

### 3.1 Data Layout (CDN Side)
The CDN hosts raw `ImmutableDB` chunk and index files:
*   `.dat`: Raw block data.
*   `.sec`: Secondary index containing block offsets and hashes.
*   `.pri`: Primary index mapping slots to secondary offsets.

### 3.2 Server-Side Implementation (`immdb-server`)

#### 3.2.1 Component: `RemoteStorage`
Responsible for the HTTP transport layer. It ensures that for a requested chunk number, the triad of files (`.dat`, `.pri`, `.sec`) is present in the local cache directory.

#### 3.2.2 Component: `OnDemand` (Unified Logic)
A single module responsible for orchestrating the download and serving of historical data. It acts as a decorator for the `ImmutableDB` record, overriding the `stream_` function.

**Key Responsibilities:**
1.  **Chunk Management:** Translates requested slot ranges into a list of required chunks.
2.  **Cache Eviction (LRU):** Ensures the disk usage for downloaded chunks stays within the configured limit by deleting the least recently used chunks before downloading new ones.
3.  **Direct Iteration (Raw Chunk Iterator):** Instead of relying on a separate module, `OnDemand` now directly implements the logic to read blocks from the disk. It:
    *   Reads the `.sec` index file to locate block boundaries.
    *   Reads the `.dat` chunk file to extract raw block bytes.
    *   Implements the Ouroboros `Iterator` interface (Next/HasNext/Close).

This unified approach reduces architectural complexity by keeping the decision to download a chunk and the logic to read that chunk tightly coupled in one stateless manager.

---

## 4. Implementation Plan

### Phase 1: Dependencies & Infrastructure
*   Verify `http-client` and `http-client-tls` integration.

### Phase 2: Core Logic
*   **Step 2.1:** Implement `RemoteStorage.hs` for chunk downloads.
*   **Step 2.2:** Implement `OnDemand.hs`. This unified module now contains:
    *   The interception logic for `ImmutableDB`.
    *   The LRU caching and download orchestration.
    *   The internal `mkRawChunkIterator` helper (previously `LightweightIterator`) to parse files directly.

### Phase 3: CLI & Wiring
*   Expose `--cdn-url` in `immdb-server.hs`.

---

## 5. Security and Robustness

*   **Isolation:** The "Bypass & Read" strategy ensures that even if a download is corrupted or a CDN file is malformed, the main `ImmutableDB` remains untouched and healthy.
*   **P2P Protocol Compliance:** The raw chunk iterator produces the exact same data stream as a standard node, ensuring full compatibility with existing syncing nodes.