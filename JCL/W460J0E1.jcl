//W460J0E1 JOB (670W4600100W460J0E1,W100),'RTN W460E1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* EXPEDITER=&VCOM                                                             
//* COUNTRYX2=&COUNTRYX2                                                        
//*                                                                             
//VCOM EXEC W016RECV                                                            
&VCOM                                                                           
W460.*.W46001 VB 254                                                            
//*                                                                             
//CTRYX2  EXEC PGM=SORT                                                         
//SYSOUT  DD   SYSOUT=*                                                         
//SORTIN  DD   DSN=&&SOPPARM,DISP=(OLD,KEEP)                                    
//SORTOUT DD   DSN=&&SOPCTRY,DISP=(NEW,PASS,DELETE),DATACLAS=PSEN               
//SYSIN   DD   *                                                                
  SORT FIELDS=COPY                                                              
  OUTFIL FNAMES=SORTOUT,                                                        
         OVERLAY=(11:13,2,1:C'COUNTRYX2(',13:C')    ')                          
/*                                                                              
//ORDER   EXEC WSOP                                                             
//W98022D1 DD  *                                                                
  ORDER W460S3 SYMBOLS                                                          
    VCOM(&VCOM)                                                                 
//   DD DSN=&&SOPPARM,DISP=(OLD,DELETE)                                         
//   DD DSN=&&SOPCTRY,DISP=(OLD,DELETE)                                         
//   DD *                                                                       
  END-ORDER                                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W460J0E1                                         
