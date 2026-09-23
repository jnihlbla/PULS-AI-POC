//W560V1RS JOB (650W5600100W560V1RS,W100),'RTN W560V1',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK  EXEC WBLOCK,NAME=W560V1                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W560.W560D1.W56019',                                           
//           T1='W560.W560V1.W56019',RF1=VB,LR1=158,CP1=15,                     
//*                                                                             
//           F2='W513.W510D2.W5134R',                                           
//           T2='W513.W560V1.W5134R',RF2=VB,LR2=059,CP2=15                      
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W560V1RS                                         
