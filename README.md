# 🔬 Lab Resource Sharing Smart Contract

A decentralized platform for sharing laboratory resources and equipment using the Stacks blockchain.

## 🎯 Features

- 📋 Register lab resources with hourly rates
- 💰 Deposit STX tokens for bookings
- 📅 Book lab resources for specific time slots
- ✅ Complete sessions and transfer payments
- 🔍 View resource details and bookings

## 🚀 Usage

### For Lab Owners

1. Add a new lab resource:
```clarity
(contract-call? .lab-sharing add-lab-resource "Microscope X1000" u100)
```

2. Complete a session after usage:
```clarity
(contract-call? .lab-sharing complete-session u1 u1234567)
```

### For Users

1. Deposit tokens:
```clarity
(contract-call? .lab-sharing deposit-tokens u1000)
```

2. Book a resource:
```clarity
(contract-call? .lab-sharing book-resource u1 u1234567 u2)
```

3. Check resource availability:
```clarity
(contract-call? .lab-sharing get-resource u1)
```

## 💡 Smart Contract Functions

- `add-lab-resource`: Register new lab equipment
- `deposit-tokens`: Add STX tokens to user balance
- `book-resource`: Reserve lab equipment
- `complete-session`: Finish usage and process payment
- `get-resource`: View resource details
- `get-booking`: Check booking information
- `get-user-balance`: Check user's token balance

## 🔒 Security

- Owner-only functions for resource management
- Secure token handling
- Booking validation checks
- Balance verification

## 🤝 Contributing


