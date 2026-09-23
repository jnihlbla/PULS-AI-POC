//W223D1MT JOB (640W2230100W223D1MT,W100),'RTN W223D1',                         
//* ÄR UPPDRAGSKODEN OVAN RÄTT?????                                             
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//TST23  EXEC WEMPTST,DSIN=W223.W223D1.W22311(+0)                               
//ACT    EXEC WSOP,COND=(0,LT,TST23.T),COMMAND='ACTIVATE W223D1MS'              
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W223D1MT                                         
