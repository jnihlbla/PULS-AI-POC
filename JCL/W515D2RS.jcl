//W515D2RS JOB (650W5100100W515D2RS,W100),'RTN W515D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK  EXEC WBLOCK,NAME=W515D2                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W510.W510D2.W51042',                                           
//           T1='W510.W515D2.W51042',RF1=FB,LR1=265,CP1=15,                     
//*                                                                             
//           F2='W418.W418D2.W41831',                                           
//           T2='W418.W515D2.W41831',RF2=FB,LR2=265,CP2=15,                     
//*                                                                             
//           F3='W476.W476D5.W4768F',                                           
//           T3='W476.W515D2.W4768F',RF3=FB,LR3=265,CP3=15,                     
//*                                                                             
//           F4='W510.W510D2.W5104B',                                           
//           T4='W510.W515D2.W5104B',RF4=FB,LR4=265,CP4=15                      
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W515D2RS                                         
