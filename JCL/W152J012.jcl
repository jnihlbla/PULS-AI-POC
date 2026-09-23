//W152J012 JOB (670W1520100W152J012,W100),'RTN W152B1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*  DETTA JOBB BEHANDLAR OCH UPDDATERAR                                        
//*  WDD3 MED ÖVERSATTA BENÄMNINGAR FRÅN CBG-KONSULT,                           
//*                                                                             
//W152    EXEC W152P012,                                                        
//             GEN='(+0)'                                                       
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W152J012                                         
