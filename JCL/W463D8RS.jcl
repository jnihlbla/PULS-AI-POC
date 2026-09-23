//W463D8RS JOB (640W4630100W463D8RS,W100),'RTN W463D8',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W463D8                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME02 EXEC WRTNINP,                                                        
//*                                                                             
//           F7='W463.W463X6JP.W46370',                                         
//           T7='W463.W463D8.JP.W46370',RF7=FB,LR7=204                          
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME03 EXEC WRTNINP,                                                        
//*                                                                             
//           F2='W463.W463X6MQ.W46370',                                         
//           T2='W463.W463D8.MQ.W46370',RF2=FB,LR2=204,                         
//*                                                                             
//           F4='W463.W463D6.W46365',                                           
//           T4='W463.W463D8.W46365',RF4=FB,LR4=190                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W463D8RS                                         
