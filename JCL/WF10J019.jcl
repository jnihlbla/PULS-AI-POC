//WF10J019 JOB (640WF100100WF10J019,W100),'RTN WF10D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE - PREL. INCLUDE                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//WF1019 EXEC WF10P019                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF10J019,                                        
//             SOPREG='W.QASE.SOP',                                             
//             PDSLIB='W.QASE.JCL',PDSTEMP='W.PRODTMP.JCL'                      
