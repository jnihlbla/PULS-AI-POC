//W553J002 JOB (640W5530100W553J002,W100),'RTN W553B2',                         
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
//             PATHIN='/app/vccs/qase/w553/data/w55302.csv',                    
//             LRECL=67,RECFM=FB,                                               
//             DSOUT=W553.W553B2.CSV.W55302(+1)                                 
//*                                                                             
//* - - - - - - - - - - Run preparation  - - - - - - - - -                      
//*                         This proc only works together with                  
//W553    EXEC W553P002     PREVIOUS 'UNIX' STEP  /C.E.                         
//*                                                                             
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W553.W553B2.W55302F(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W553.W553B2.W55302F(+1)                                   
//    ENDIF                                                                     
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W553J002                                         
