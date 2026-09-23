//W553J004 JOB (640W5530100W553J004,W100),'RTN W553B3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
//      INCLUDE MEMBER=SYSTZ                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//* Job started by &MAILID                                                      
//*                     in WEB-appl W55301 (Finance/Classic)                    
//*                                                                             
//* - - - - - - - - - - Get File From Server - - - - - - -                      
//UNIX    EXEC W001HFSG,                                                        
//             PATHIN='/app/vccs/qase/w553/data/w55304.csv',                    
//             LRECL=150,RECFM=FB,                                              
//             DSOUT=W553.W553B3.CSV.W55304(+1)                                 
//*                                                                             
//* - - - - - - - - - - Run preparation  - - - - - - - - -                      
//*                         This proc only works together with                  
//W553    EXEC W553P004     PREVIOUS 'UNIX' STEP  /C.E.                         
//*                                                                             
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W553.W553B3.W55304F(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W553.W553B3.W55304F(+1)                                   
//    ELSE                                                                      
//      EXEC PGM=IEFBR14                                                        
//DD1   DD DSN=W553.W553B3.W55304F(+1),DISP=(OLD,DELETE)                        
//    ENDIF                                                                     
//*                                                                             
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W553J004                                         
