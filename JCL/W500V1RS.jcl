//W500V1RS JOB (650W5100100W500V1RS,W100),'RTN W500V1',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK  EXEC WBLOCK,NAME=W500V1                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W510.W510D4.W51068',                                           
//           T1='W510.W500V1.W51068',RF1=FB,LR1=083,CP1=5,                      
//*                                                                             
//           F2='W510.W510D4.W5106D',                                           
//           T2='W510.W500V1.W5106D',RF2=FB,LR2=083,CP2=5,                      
//*                                                                             
//           F3='W510.W510D4.W51081',                                           
//           T3='W510.W500V1.W51081',RF3=FB,LR3=083,CP3=5,                      
//*                                                                             
//           F4='W510.W510D5.W51078',                                           
//           T4='W510.W500V1.W51078',RF4=VB,LR4=064,CP4=9,                      
//*                                                                             
//           F5='WUT.W510D4.W51080',                                            
//           T5='W510.W500V1.W51080',RF5=FB,LR5=300,CP5=5,                      
//*                                                                             
//           F6='W541.W510D3.W54105',                                           
//           T6='W541.W500V1.W54105',RF6=FB,LR6=077                             
//SOP     EXEC WSOPEND,PROCESS=W500V1RS                                         
