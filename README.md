# Escrow Contract for Property Purchase
A secure and transparent escrow system for property purchases on the Stacks blockchain.

## 🎯 Features

- Secure deposit handling
- Multi-step verification process
- Automated fund release
- Inspection status tracking
- Title clearance verification
- Mortgage approval tracking
- Deadline-based execution
- Escrow fee management

## 💡 How It Works

1. **Creating an Escrow**
   - Seller initiates the escrow
   - Specifies property ID, buyer, price, deposit, and deadline
   - Deposit is locked in the contract

2. **Verification Steps**
   - Property inspection status
   - Title clearance verification
   - Mortgage approval confirmation

3. **Completion**
   - Buyer completes purchase when all conditions are met
   - Funds are automatically distributed
   - Escrow fee is sent to contract owner

4. **Cancellation**
   - Available after deadline expires
   - Deposit returned to buyer

## 🛠 Usage

### Creating an Escrow

```clarity
(contract-call? .escrow-contract create-escrow u1 'BUYER-ADDRESS u100000 u10000 u1000)
```

### Updating Verification Statuses

```clarity
(contract-call? .escrow-contract update-inspection-status u1 true)
(contract-call? .escrow-contract update-title-status u1 true)
(contract-call? .escrow-contract update-mortgage-status u1 true)
```

### Completing Purchase

```clarity
(contract-call? .escrow-contract complete-purchase u1)
```

## ⚙️ Configuration

- Default escrow fee: 2%
- Minimum deposit: 1000 STX

## 🔒 Security

- Role-based access control
- Deadline enforcement
- Secure fund handling
```
