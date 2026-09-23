//W271D8RS JOB (640W2710100W271D8RS,W100),'RTN W271D8',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W271D8                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W271.DUMMY.W27156',                                            
//           T1='W271.W271D8.W27156',RF1=FB,LR1=041,                            
//*                                                                             
//           F2='W271.DUMMY.W27158',                                            
//           T2='W271.W271D8.W27158',RF2=FB,LR2=041                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271D8RS                                         
