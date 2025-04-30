# 📊 Blockchain Census Platform

A privacy-respecting population registration and statistics system built on Stacks blockchain.

## 🎯 Features

- ✨ Secure citizen registration
- 📍 Regional population tracking
- 📈 Automated demographics calculation
- 🔒 Admin-controlled access
- 📊 Real-time statistics

## 🚀 Usage

### Administrative Functions

```clarity
(contract-call? .censu register-citizen "John Doe" u1990 "New York")
(contract-call? .censu update-citizen-status u1 "inactive")
(contract-call? .censu set-admin tx-sender)
```

### Read-Only Functions

```clarity
(contract-call? .censu get-citizen u1)
(contract-call? .censu get-region-stats "New York")
(contract-call? .censu get-total-citizens)
```

## 🔑 Security

- Only authorized admin can register citizens and update statuses
- Immutable registration records
- Privacy-preserving statistical aggregation

## 🛠 Requirements

- Clarinet
- Stacks blockchain wallet
- Administrative access for write operations

## 📝 License

MIT

