//W570D4RS JOB (650W5700100W570D4RS,W100),'RTN W570D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK  EXEC WBLOCK,NAME=W570D4                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W570.W570D3.W57074',                                           
//           T1='W570.W570D4.W57074',RF1=VB,LR1=755,CP1=10,                     
//*                                                                             
//           F2='W570.W570D3.W57084',                                           
//           T2='W570.W570D4.W57084',RF2=VB,LR2=755,CP2=10,                     
//*                                                                             
//           F3='W570.W570D3.W5702O',                                           
//           T3='W570.W570D4.W5702O',RF3=FB,LR3=121,CP3=15,                     
//*                                                                             
//           F4='W570.W570D3.W5702P',                                           
//           T4='W570.W570D4.W5702P',RF4=FB,LR4=121,CP4=15,                     
//*                                                                             
//           F5='W570.W570D3.W5707O',                                           
//           T5='W570.W570D4.W5707O',RF5=FB,LR5=121,CP5=15,                     
//*                                                                             
//           F6='W570.W570D3.W5707P',                                           
//           T6='W570.W570D4.W5707P',RF6=FB,LR6=121,CP6=15                      
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W570D4RS                                         
