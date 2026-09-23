//W513D3RS JOB (650W5130100W513D3RS,W100),'RTN W513D3',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W513D3                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W513.W510D2.W51334',                                           
//           T1='W513.W513D3.W51334',RF1=FB,LR1=74,                             
//*                                                                             
//           F2='W513.W510D2.W51335',                                           
//           T2='W513.W513D3.W51335',RF2=FB,LR2=55,                             
//*                                                                             
//           F3='W513.W510D2.W51336',                                           
//           T3='W513.W513D3.W51336',RF3=FB,LR3=55                              
//SOP     EXEC WSOPEND,PROCESS=W513D3RS                                         
