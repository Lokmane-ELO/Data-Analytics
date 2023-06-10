import matplotlib.pyplot as plt

df = pd.read_csv('data1TP1.txt', sep='\t')  # read the data file
plt.scatter(df['A'], df['Y'])
plt.xlabel('A')
plt.ylabel('Y')
plt.show()