//W114D5RS JOB (640W1140100W114D5RS,W100),'RTN W114D5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W114D5                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W217.W200D1.W21707',                                           
//           T1='W217.W114D5.W21707',RF1=FB,LR1=080,                            
//*                                                                             
//           F2='W111.W111D1.W11146',                                           
//           T2='W111.W114D5.W11146',RF2=FB,LR2=080,                            
//*                                                                             
//           F3='W092.W092D6.W092ZR',                                           
//           T3='W092.W114D5.W092ZR',RF3=FB,LR3=080,                            
//*                                                                             
//           F4='W216.W216D2.W21634',                                           
//           T4='W216.W114D5.W21634',RF4=FB,LR4=009                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W114D5RS                                         
