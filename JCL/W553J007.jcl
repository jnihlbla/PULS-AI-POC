//W553J007 JOB (640W5530100W553J007,W100),'RTN W553B4',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//* Job started by &MAILID                                                      
//*                     in WEB-appl W55307(Finance/Classic)                     
//*                                                                             
//* - - - - - - - - - - Get File From Server - - - - - - -                      
//UNIX    EXEC W001HFSG,                                                        
//             PATHIN='/app/vccs/qase/w553/data/w55307.csv',                    
//             LRECL=150,RECFM=FB,                                              
//             DSOUT=W553.W553B4.CSV.W55307(+1)                                 
//*                                                                             
//* - - - - - - - - - - Run preparation  - - - - - - - - -                      
//*                         This proc only works together with                  
//W553    EXEC W553P007     PREVIOUS 'UNIX' STEP  /C.E.                         
//*                                                                             
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W553.W553B4.W55307F(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W553.W553B4.W55307F(+1)                                   
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W553J007                                         
