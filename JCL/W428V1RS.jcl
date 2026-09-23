//W428V1RS JOB (640W4280100W428V1RS,W100),'RTN W428V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W428V1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W428.W428D1.W42834',                                           
//           T1='W428.W428V1.W42834',RF1=FB,LR1=22,                             
//*                                                                             
//           F2='W418.W418D2.W418AN',                                           
//           T2='W418.W428V1.W418AN',RF2=FB,LR2=71,CP2=6,                       
//*                                                                             
//           F3='W418.W418S2.W418AM',                                           
//           T3='W418.W428V1.W418AM',RF3=FB,LR3=71,CP3=6                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W428V1RS                                         
