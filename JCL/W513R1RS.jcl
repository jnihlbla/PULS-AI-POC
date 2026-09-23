//W513R1RS JOB (650W5130100W513R1RS,W100),'RTN W513R1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W513R1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W513.W513V1.W51326',                                           
//           T1='W513.W513R1.W51326',RF1=VB,LR1=081,                            
//*                                                                             
//           F2='W513.W513V1.W51360',                                           
//           T2='W513.W513R1.W51360',RF2=FB,LR2=020                             
//SOP     EXEC WSOPEND,PROCESS=W513R1RS                                         
