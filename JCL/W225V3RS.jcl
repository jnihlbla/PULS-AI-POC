//W225V3RS JOB (640W2250100W225V3RS,W100),'RTN W225V3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINECT=0,FORMS=1800                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WBLOCK EXEC WBLOCK,NAME=W225V3                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W231.W231V3.W23196',                                           
//           T1='W231.W225V3.W23196',RF1=FB,LR1=787                             
//*                                                                             
//SOP    EXEC WSOPEND,PROCESS=W225V3RS                                          
/*                                                                              
