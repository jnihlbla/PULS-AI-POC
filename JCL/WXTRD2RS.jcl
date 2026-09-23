//WXTRD2RS JOB (650W0001000WXTRD2RS,W100),'RTN WXTRD2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=WXTRD2                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG                               
//*                                                                             
//*********************************************************************         
//RENAME01 EXEC WRTNINP,                                                        
//           F1='W612.W612DA.W61271A',                                          
//           T1='WXTR.WXTRD2.W61271A',RF1=FB,LR1=11                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WXTRD2RS                                         
