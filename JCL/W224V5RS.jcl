//W224V5RS JOB (650W2240100W224V5RS,W100),'RTN W224V5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W224V5                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W271.W271V3.W27178',                                           
//           T1='W271.W224V5.W27178',RF1=FB,LR1=026,                            
//*                                                                             
//           F2='W221.W221V4.W22168',                                           
//           T2='W221.W224V5.W22168',RF2=FB,LR2=012                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W224V5RS                                         
