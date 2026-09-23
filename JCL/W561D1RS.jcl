//W561D1RS JOB (650W5100100W561D1RS,W100),'RTN W561D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W561D1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//RENAME01 EXEC WRTNINP,                                                        
//           F1='WIN.W561X1US.W56112',                                          
//           T1='W561.W561D1.W56112',RF1=VB,LR1=1054,CP1=5                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W561D1RS                                         
