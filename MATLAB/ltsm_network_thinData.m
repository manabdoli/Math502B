%% Read Noisy Masks and Responses
thinNoisyMasks=readtable('Noisy100ThinData.csv');
thinNoisyMasks=thinNoisyMasks{:,:};
[nr, nc] = size(thinNoisyMasks);
thinNoisyMasks = mat2cell(thinNoisyMasks, ones(nr, 1), 361);

nRow = 31;
thinLocIdx=readtable('Noisy100ThinData_y.csv');
thinLocIdx = thinLocIdx{:,:};
[thinLocX, thinLocY] = index2pos(thinLocIdx, nRow);

thinLocIdxC=discretize(thinLocIdx, (0:961)+.5, 'categorical');
thinLocIdx=num2cell(thinLocIdx);
thinLocIdxC=num2cell(thinLocIdxC);


%% Train & Test
numObs = size(thinNoisyMasks,1);                         % This will provide how many masks we have in the data set
Naz = size(thinNoisyMasks,2);                            % Number of azimuthal angles


[idxTrain, idxTest] = trainingPartitions(numObs, [0.8 0.2]); % partition the data into training and testing data needs integer for first argument


%% 

%A = categorical(thinLocIdx,"Ordinal",true);     % create categories of possible output responses

XTrain = thinNoisyMasks(idxTrain);
YTrain = thinLocIdx(idxTrain);
CTrain = thinLocIdxC(idxTrain);

XTest = thinNoisyMasks(idxTest);
YTest = thinLocIdx(idxTest);
CTest = thinLocIdxC(idxTest);

%%
numHiddenUnits1 = 241;
numHiddenUnits2 = 162;                                      % play around with number of hidden units
numClasses = 961;                              % This is correlated to the number of pixels we have
numchannel = 1;                                          % set numchannels as number of inputs (The third dimension; similar to the forth in image data)

layers = [ ...
    sequenceInputLayer(numchannel)
    lstmLayer(numHiddenUnits1,'OutputMode','sequence')
    dropoutLayer(0.2)
    lstmLayer(numHiddenUnits2,'OutputMode',"last")
    %Convolution Layer Here
    fullyConnectedLayer(numClasses)
    softmaxLayer
    classificationLayer];

options = trainingOptions("adam", ...
    MaxEpochs=150, ... 
    InitialLearnRate=0.01,...
    Shuffle="every-epoch", ...
    GradientThreshold=1, ...
    Verbose=false, ...
    Plots='training-progress');
disp('Layers and Options are set!')
%%
net = trainNetwork(XTrain,CTrain,layers,options);
%%
