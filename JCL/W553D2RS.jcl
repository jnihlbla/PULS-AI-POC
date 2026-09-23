//W553D2RS JOB (650W5530100W553D2RS,W100),'RTN W553D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W553D2                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W553.DUMMY.W55316',                                            
//           T1='W553.W553D2.W55316',RF1=FB,LR1=35,                             
//*                                                                             
//           F2='W092.W092D2.W09286',                                           
//           T2='W092.W553D2.W09286',RF2=FB,LR2=35,CP2=9,                       
//*                                                                             
//           F3='W553.W553B1.W55301',                                           
//           T3='W553.W553D2.W55301',RF3=FB,LR3=35,                             
//*                                                                             
//           F4='W161.W161S2.W16153',                                           
//           T4='W553.W553D2.W16153',RF4=FB,LR4=35                              
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W553D2RS                                         
