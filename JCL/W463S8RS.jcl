//W463S8RS JOB (540W4630100W463S8RS,W100),'RTN W463S8',                         
//             CLASS=K,                                                         
//             USER=?,PASSWORD=?                                                
/*JOBPARM FORMS=1800,LINECT=0                                                   
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W463S8                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='WIN.W463X3SE.W46383',                                          
//           T1='W463.W463S8.W46383',RF1=VB,LR1=1009                            
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W463S8RS                                         
