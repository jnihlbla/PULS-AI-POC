//W570D2RS JOB (650W5700100W570D2RS,W100),'RTN W570D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK  EXEC WBLOCK,NAME=W570D2                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W510.W510D2.W51041',                                           
//           T1='W510.W570D2.W51041',RF1=FB,LR1=265,CP1=15,                     
//*                                                                             
//           F2='W418.W418D2.W41832',                                           
//           T2='W418.W570D2.W41832',RF2=FB,LR2=265,CP2=15,                     
//*                                                                             
//           F3='W476.W476D5.W4768E',                                           
//           T3='W476.W570D2.W4768E',RF3=FB,LR3=265,CP3=15,                     
//*                                                                             
//           F4='W510.W510D2.W5104A',                                           
//           T4='W510.W570D2.W5104A',RF4=FB,LR4=265,CP4=15,                     
//*                                                                             
//           F5='W426.W426V1.W42634',                                           
//           T5='W426.W570D2.W42634',RF5=FB,LR5=265,CP5=15,                     
//*                                                                             
//           F6='W510.W510D2.W51047',                                           
//           T6='W510.W570D2.W51047',RF6=FB,LR6=265,CP6=15,                     
//*                                                                             
//           F7='W510.W510D2.W5104D',                                           
//           T7='W510.W570D2.W5104D',RF7=FB,LR7=265,CP7=15                      
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W570D2RS                                         
