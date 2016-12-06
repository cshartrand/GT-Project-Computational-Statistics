%Clustering Analysis of What Factors Contribute to Higher Average Salary of a Colleges Graduates Based on Students 6 years and 10 years out from Initial Enrollment 
%%For 6 year data
data = importdata('clusterdata6yr.xlsx');
X6 = data(:,2:19);
k=3;
[idx,C] = kmeans(X6,k);
beta = importdata('beta6yr.txt');
beta = beta.data;
C = [ones(3,1) C];
Center = C*beta;
%%In the case of the saved plots, Cluster 1 = 30, Cluster 2 = 38, Cluster 3
%%= 22
figure;
plot(X6(idx==1,4),X6(idx==1,1),'r.','MarkerSize',12)
hold on
plot(X6(idx==2,4),X6(idx==2,1),'b.','MarkerSize',12)
plot(X6(idx==3,4),X6(idx==3,1),'g.','MarkerSize',12)
legend('Cluster 1','Cluster 2','Cluster 3',...
       'Location','NW')
title '6 Year Assignments on Instructional Expenditures by Average Family Income of Dependent Students'
hold off

figure;
plot(X6(idx==1,12),X6(idx==1,9),'r.','MarkerSize',12)
hold on
plot(X6(idx==2,12),X6(idx==2,9),'b.','MarkerSize',12)
plot(X6(idx==3,12),X6(idx==3,9),'g.','MarkerSize',12)
legend('Cluster 1','Cluster 2','Cluster 3',...
       'Location','NW')
title ' 6 Year Assignments on Net Tuition Revenue by Cumulative Loan Debt at the 25th Percentile'
hold off

figure;
plot(X6(idx==1,5),X6(idx==1,11),'r.','MarkerSize',12)
hold on
plot(X6(idx==2,5),X6(idx==2,11),'b.','MarkerSize',12)
plot(X6(idx==3,5),X6(idx==3,11),'g.','MarkerSize',12)
legend('Cluster 1','Cluster 2','Cluster 3',...
       'Location','NW')
title '6 Year Assignments on % of Engineering Degrees Awarded by % of Visual Arts Degrees Awarded'
hold off

figure;
plot(X6(idx==1,5),X6(idx==1,8),'r.','MarkerSize',12)
hold on
plot(X6(idx==2,5),X6(idx==2,8),'b.','MarkerSize',12)
plot(X6(idx==3,5),X6(idx==3,8),'g.','MarkerSize',12)
legend('Cluster 1','Cluster 2','Cluster 3',...
       'Location','NW')
title '6 Year Assignments on % of Engineering Degrees Awarded by Share of Undergrads Who Are Asian'
hold off

[coeff,score,latent] = pca(X6);
cumsum(latent)./sum(latent)
kmeanPCA = coeff(:,1:3);
kmeanScore = score(:,1:3);
[idx2,C2] = kmeans(kmeanScore,k);
%PC1 is on dependents family income, PC2 is on independents family income,
%PC3 is on tuition revenue
figure;
scatter3(kmeanScore(idx2==1,1),kmeanScore(idx2==1,2),kmeanScore(idx2==1,3),'r.')
hold on
scatter3(kmeanScore(idx2==2,1),kmeanScore(idx2==2,2),kmeanScore(idx2==2,3),'b.')
scatter3(kmeanScore(idx2==3,1),kmeanScore(idx2==3,2),kmeanScore(idx2==3,2),'g.')
legend('Cluster 1','Cluster 2','Cluster 3',...
       'Location','NW')
title '6 Year Cluster Assignments on First 3 Principal Components'
hold off

%%Now for 10 year data
data10 = importdata('clusterdata10yr.xlsx');
X10 = data10(:,2:19);
k=3;
[index,Clus] = kmeans(X10,k);
beta10 = importdata('beta10yr.xlsx');
Clus = [ones(3,1) Clus];
Center10 = Clus*beta10;
%%Clusters are 27,37,48 will indicate which ones when graphs are produced
%%C1=37,C2=48,C3=27

[coeff10,score10,latent10] = pca(X10);
%%PC1 is dependent family income, PC2 is independent family income, PC3 is
%%expenditure on full time student
cumsum(latent10)./sum(latent10)
kmeanPCA10 = coeff10(:,1:3);
kmeanScore10 = score10(:,1:3);
[index10,C2] = kmeans(kmeanScore,k);
figure;
scatter3(kmeanScore10(index10==1,1),kmeanScore10(index10==1,2),kmeanScore10(index10==1,3),'r.')
hold on
scatter3(kmeanScore10(index10==2,1),kmeanScore10(index10==2,2),kmeanScore10(index10==2,3),'b.')
scatter3(kmeanScore10(index10==3,1),kmeanScore10(index10==3,2),kmeanScore10(index10==3,2),'g.')
legend('Cluster 1','Cluster 2','Cluster 3',...
       'Location','NW')
title '10 Year Cluster Assignments on First 3 Principal Components'
hold off

figure;
plot(X10(index==1,7),X10(index==1,1),'r.','MarkerSize',12)
hold on
plot(X10(index==2,7),X10(index==2,1),'b.','MarkerSize',12)
plot(X10(index==3,7),X10(index==3,1),'g.','MarkerSize',12)
legend('Cluster 1','Cluster 2','Cluster 3',...
       'Location','NW')
title 'Cluster Assignments for 10 Years on Instructional Expenditures per Full-Time Student by Average Family Income of Dependent Students'
hold off

%%Slightly more analysis for 6 years
X6F = data(:,1:19);
mean6 = mean(data(:,1));
class6 =[];
for i=1:3046
    if(X6F(i,1)< mean6)
        class6 = [class6 0];
    else
        class6 = [class6 1];
    end
end
X6F = [X6F class6'];
X10F = data10(:,1:19);
mean10 = mean(data(:,1));
class10 =[];
for i=1:3046
    if(X6F(i,1)< mean6)
        class10 = [class10 0];
    else
        class10 = [class10 1];
    end
end
X10F = [X10F class10'];
csvwrite('toyyalog6',X6F);
csvwrite('toyyalog10',X10F);