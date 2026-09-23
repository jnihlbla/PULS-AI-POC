//W271J01H JOB (640W2710100W271J01H,W100),'RTN W271S2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
//      INCLUDE MEMBER=SYST0                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//* SOP-PARAMETERS FOR THIS JOB:                                                
//*     DC    : &DC                                                             
//*     OPTION: &OPTION                                                         
//*     EMAIL : &EMAIL                                                          
//*                                                                             
//W271    EXEC W271P01H                                                         
//*                                                                             
//*                                                                             
//COPY    EXEC PGM=ICEGENER                                                     
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DUMMY                                                            
//SYSUT1   DD  *                                                                
Selection criteria: DC &DC  Option: &OPTION 				                                
//*the line above has 4 tabs (hex 05) here  ^^^^                                
//SYSUT2   DD  DSN=W271.W271S2.W2711H.SELCRIT(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             DCB=(RECFM=FB,LRECL=162),                                        
//             MGMTCLAS=BACKUPC,DATACLAS=PSEN                                   
//*                                                                             
//*                                                                             
//TZIP    EXEC WZ11TZIP,                                                        
//            DSIN=W271.W271S2.W2711H(+1),                                      
//            DSOUTZIP=&&W2711H,                                                
//            ZIPDISP=(NEW,PASS,DELETE),                                        
//            CONTENT=W2711H.XLS                                                
//ICONV.SYSUT1 DD  DSN=W271.W271S2.W2711H.SELCRIT(+1),DISP=SHR                  
//        DD DSN=W271.W271S2.W2711H.HEADERS,DISP=SHR                            
//        DD DSN=W271.W271S2.W2711H(+1),DISP=SHR                                
//*                                                                             
//*                                                                             
//MAIL    EXEC WMAILSND,COND.MABEND=(0,LE)                                      
//TEMPOUT   DD DSN=&&W2711H,DISP=(OLD,DELETE)                                   
)SEND                                                                           
TITLE  Refill proposals for DC &DC, Option &OPTION                              
TO     &EMAIL                                                                   
ATTACH TEMPOUT  W2711H.ZIP BIN                                                  
MAIL                                                                            
 REFILL PROPOSAL INFO                                                           
 ORDERED FROM SCREEN 2373                                                       
)END                                                                            
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J01H                                         
