//W216V1RS JOB (650W2160100W216V1RS,W100),'RTN W216V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK  EXEC WBLOCK,NAME=W216V1                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W216.W216D2.W21637',                                           
//           T1='W216.W216V1.W21637',RF1=FB,LR1=037,                            
//*                                                                             
//           F2='W335.W335V1.W3350F',                                           
//           T2='W335.W216V1.W3350F',RF2=FB,LR2=047                             
//SOP     EXEC WSOPEND,PROCESS=W216V1RS                                         
