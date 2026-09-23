//W463S4RS JOB (540W4630100W463S4RS,W100),'RTN W463S4',                         
//             CLASS=K,                                                         
//             USER=?,PASSWORD=?                                                
/*JOBPARM FORMS=1800,LINECT=0                                                   
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
/*CNTL W46342,EXC                                                               
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W463S4                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='WIN.W463X7SE.W46342',                                          
//           T1='W463.W463S4.W46342',RF1=VB,LR1=1009,CP1=99                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W463S4RS                                         
