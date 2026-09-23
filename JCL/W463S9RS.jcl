//W463S9RS JOB (540W4630100W463S9RS,W100),'RTN W463S9',                         
//             CLASS=K,                                                         
//             USER=?,PASSWORD=?                                                
/*JOBPARM FORMS=1800,LINECT=0                                                   
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
/*CNTL W46355,EXC                                                               
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W463S9                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='WIN.W463X4SE.W46355',                                          
//           T1='W463.W463S9.W46355',RF1=VB,LR1=1009                            
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W463S9RS                                         
