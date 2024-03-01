%%
% openExample('nnet/SequenceToOneRegressionUsingDeepLearningExample')
load WaveformData
numChannels = size(data{1},1);

idx = [3 4 5 12];
figure
tiledlayout(2,2)
for i = 1:4
    nexttile
    stackedplot(data{idx(i)}',DisplayLabels="Channel "+string(1:numChannels))
    
    xlabel("Time Step")
    title("Class: " + string(labels(idx(i))))
end
%%
numObservations = numel(data);
[idxTrain,idxTest] = trainingPartitions(numObservations, [0.9 0.1]);
XTrain = data(idxTrain);
TTrain = labels(idxTrain);

XTest = data(idxTest);
TTest = labels(idxTest);
%%
numHiddenUnits = 120;
numClasses = numel(categories(TTrain));

layers = [ ...
    sequenceInputLayer(numChannels)
    lstmLayer(numHiddenUnits,OutputMode="last")
    fullyConnectedLayer(numClasses)
    softmaxLayer
    classificationLayer]
%%
options = trainingOptions("adam", ...
    MaxEpochs=150, ...
    InitialLearnRate=0.01,...
    Shuffle="every-epoch", ...
    GradientThreshold=1, ...
    Verbose=false, ...
    Plots="training-progress");
%%
net = trainNetwork(XTrain,TTrain,layers,options);

%%
