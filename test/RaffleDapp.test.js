describe("Testing RaffleDapp contract", () => {
  test("it should deploy the contract with correct storage", async () => {
    const storage = {};
    const contract = await deployContract('RaffleDapp.mligo', storage);

    // Grab the content of contract's storage in blockchain
    const contractStorage = await contract.storage();

    expect(Number(contractStorage)).toBe(storage);
  });
})
