//W553J001 JOB (640W5530100W553J001,W100),'RTN W553B1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
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
//             PATHIN='/app/vccs/qase/w553/data/w55301.csv',                    
//             LRECL=67,RECFM=FB,                                               
//             DSOUT=W553.W553B1.CSV.W55301(+1)                                 
//*                                                                             
//* - - - - - - - - - - Run preparation  - - - - - - - - -                      
//*                         This proc only works together with                  
//W553    EXEC W553P001     previous 'UNIX' step  /C.E.                         
//*                                                                             
//*                                                                             
//* - - - - - - - - - - Send Receipt by Mail To User - - - -                    
//IFERR   IF W553.W55301.RC=7 THEN                                              
//*                                                                             
//MEMO    EXEC WMEMOSND,DSIN=NULLFILE                                           
//APIFILE DD *                                                                  
)SEND                                                                           
  TITLE CGP Not OK                                                              
  OPTION FORCE                                                                  
  DEST &MAILID                                                                  
  MEMO MEMTXTD1                                                                 
)END                                                                            
//MEMTXTD1 DD DSN=W.QASE.CONSTANT(W55301ME),DISP=SHR                            
//         DD DSN=W553.W553B1.W55302(+1),DISP=SHR                               
//SYSABEND DD SYSOUT=*                                                          
//SYSOUT   DD SYSOUT=*                                                          
//*                                                                             
//  ELSE                                                                        
//IFOK  IF W553.W55301.RC<7 THEN                                                
//*                                                                             
//MEMO    EXEC WMEMOSND,DSIN=NULLFILE                                           
//APIFILE DD *                                                                  
)SEND                                                                           
  TITLE Classic OK                                                              
  OPTION FORCE                                                                  
  DEST &MAILID                                                                  
  MEMO                                                                          
                                                                                
  Your request to process a file with                                           
  new prices for Genuine Classic Parts                                          
  has ended OK, and will be updated in                                          
  PULS central prices tonight.                                                  
                                                                                
  If multiple uploads is performed the same day.                                
        Just only the last one will be effectuated.                             
                                                                                
)END                                                                            
//SYSABEND DD SYSOUT=*                                                          
//SYSOUT   DD SYSOUT=*                                                          
//*                                                                             
//IFOKEND  ENDIF                                                                
//*                                                                             
//IFERREND ENDIF                                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W553J001                                         
