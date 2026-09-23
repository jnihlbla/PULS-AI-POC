//W483V1RS JOB (650W4830100W483V1RS,W100),'RTN W483V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK  EXEC WBLOCK,NAME=W483V1                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W479.W479V2.W47953',                                           
//           T1='W479.W483V1.W47953',RF1=FB,LR1=167,                            
//*                                                                             
//           F2='W479.W479V2.W47955',                                           
//           T2='W479.W483V1.W47955',RF2=FB,LR2=177,                            
//*                                                                             
//           F3='W479.W479V2.W47960',                                           
//           T3='W479.W483V1.W47960',RF3=FB,LR3=81,                             
//*                                                                             
//           F4='W428.W428V1.W42856',                                           
//           T4='W428.W483V1.W42856',RF4=FB,LR4=95                              
//SOP     EXEC WSOPEND,PROCESS=W483V1RS                                         
