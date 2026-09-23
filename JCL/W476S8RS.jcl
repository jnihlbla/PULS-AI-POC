//W476S8RS JOB (640W4760100W476S8RS,W100),'RTN W476S8',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W476S8                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W476.W476S5.W47670',                                           
//           T1='W476.W476S8.W47670',RF1=FB,LR1=083,CP1=5                       
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476S8RS                                         
