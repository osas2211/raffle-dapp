describe("E2E Testing RaffleDapp contract", () => {
  const getRandomInt = () => Math.round(Math.random() * 1000);

  let contract = null;

  // Contract will be deployed just once
  beforeAll(async () => {
    const storage = getRandomInt();
    
    // If tests were launched on testenet specifying a contract address with the --contracts option,
    // NO DEPLOY WILL BE EXECUTED, but contract will just be accessed instead.
    // For this reason keep in mind that the contract storage state is something you
    // CANNOT PREDICT (unless you just deployed this contract; in that case go on)
    contract = await deployContract('RaffleDapp.mligo', storage);
  });

  test("it should deploy the contract with correct storage", async () => {
    const storage = {};

    // Grab the content of contract's storage in blockchain
    const contractStorage = await contract.storage();

    expect(Number(contractStorage)).toBe(storage);
  });
});
