//W15222FT JOB (640W1520100W15222FT,W100),'RTN W152V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//*+JBS BIND IMG0                                                               
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* Denna körning  =>  DAGID(&DAGID)                                            
//* startad med    =>  ORDBEN(&ORDBEN)                                          
//* Symbol  Values =>  ORDLEX(&ORDLEX)                                          
//****************************************************                          
//EMPTYCN EXEC WEMPTST,DSIN=W152.W152V2.W152TOCN(+0)                            
//    IF (EMPTYCN.T.RC = 0) THEN                                                
//WQSENCN EXEC WZ11P023,                                                        
//             DSIN=W152.W152V2.W152TOCN(+0)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.CBG.PARTSDESCRIPTION                                        
¤MQMPROP PhysicalId=P-%YYMMDD.cn                                                
¤MQMPROP Authorization=Basic Q0JHUFJPRDpGVFBQVUxTUFJPRA                         
/*                                                                              
//    ENDIF                                                                     
//*                                                                             
//****************************************************                          
//EMPTYDE EXEC WEMPTST,DSIN=W152.W152V2.W152TODE(+0)                            
//    IF (EMPTYDE.T.RC = 0) THEN                                                
//WQSENDE EXEC WZ11P023,                                                        
//             DSIN=W152.W152V2.W152TODE(+0)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.CBG.PARTSDESCRIPTION                                        
¤MQMPROP PhysicalId=P-%YYMMDD.de                                                
¤MQMPROP Authorization=Basic Q0JHUFJPRDpGVFBQVUxTUFJPRA                         
/*                                                                              
//    ENDIF                                                                     
//*                                                                             
//****************************************************                          
//EMPTYEN EXEC WEMPTST,DSIN=W152.W152V2.W152TOEN(+0)                            
//    IF (EMPTYEN.T.RC = 0) THEN                                                
//WQSENEN EXEC WZ11P023,                                                        
//             DSIN=W152.W152V2.W152TOEN(+0)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.CBG.PARTSDESCRIPTION                                        
¤MQMPROP PhysicalId=P-%YYMMDD.en                                                
¤MQMPROP Authorization=Basic Q0JHUFJPRDpGVFBQVUxTUFJPRA                         
/*                                                                              
//    ENDIF                                                                     
//*                                                                             
//****************************************************                          
//EMPTYES EXEC WEMPTST,DSIN=W152.W152V2.W152TOES(+0)                            
//    IF (EMPTYES.T.RC = 0) THEN                                                
//WQSENES EXEC WZ11P023,                                                        
//             DSIN=W152.W152V2.W152TOES(+0)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.CBG.PARTSDESCRIPTION                                        
¤MQMPROP PhysicalId=P-%YYMMDD.es                                                
¤MQMPROP Authorization=Basic Q0JHUFJPRDpGVFBQVUxTUFJPRA                         
/*                                                                              
//    ENDIF                                                                     
//*                                                                             
//****************************************************                          
//EMPTYFI EXEC WEMPTST,DSIN=W152.W152V2.W152TOFI(+0)                            
//    IF (EMPTYFI.T.RC = 0) THEN                                                
//WQSENFI EXEC WZ11P023,                                                        
//             DSIN=W152.W152V2.W152TOFI(+0)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.CBG.PARTSDESCRIPTION                                        
¤MQMPROP PhysicalId=P-%YYMMDD.fi                                                
¤MQMPROP Authorization=Basic Q0JHUFJPRDpGVFBQVUxTUFJPRA                         
/*                                                                              
//    ENDIF                                                                     
//*                                                                             
//****************************************************                          
//EMPTYFR EXEC WEMPTST,DSIN=W152.W152V2.W152TOFR(+0)                            
//    IF (EMPTYFR.T.RC = 0) THEN                                                
//WQSENFR EXEC WZ11P023,                                                        
//             DSIN=W152.W152V2.W152TOFR(+0)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.CBG.PARTSDESCRIPTION                                        
¤MQMPROP PhysicalId=P-%YYMMDD.fr                                                
¤MQMPROP Authorization=Basic Q0JHUFJPRDpGVFBQVUxTUFJPRA                         
/*                                                                              
//    ENDIF                                                                     
//*                                                                             
//****************************************************                          
//EMPTYIT EXEC WEMPTST,DSIN=W152.W152V2.W152TOIT(+0)                            
//    IF (EMPTYIT.T.RC = 0) THEN                                                
//WQSENIT EXEC WZ11P023,                                                        
//             DSIN=W152.W152V2.W152TOIT(+0)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.CBG.PARTSDESCRIPTION                                        
¤MQMPROP PhysicalId=P-%YYMMDD.it                                                
¤MQMPROP Authorization=Basic Q0JHUFJPRDpGVFBQVUxTUFJPRA                         
/*                                                                              
//    ENDIF                                                                     
//*                                                                             
//****************************************************                          
//EMPTYJA EXEC WEMPTST,DSIN=W152.W152V2.W152TOJA(+0)                            
//    IF (EMPTYJA.T.RC = 0) THEN                                                
//WQSENJA EXEC WZ11P023,                                                        
//             DSIN=W152.W152V2.W152TOJA(+0)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.CBG.PARTSDESCRIPTION                                        
¤MQMPROP PhysicalId=P-%YYMMDD.ja                                                
¤MQMPROP Authorization=Basic Q0JHUFJPRDpGVFBQVUxTUFJPRA                         
/*                                                                              
//    ENDIF                                                                     
//*                                                                             
//****************************************************                          
//EMPTYKO EXEC WEMPTST,DSIN=W152.W152V2.W152TOKO(+0)                            
//    IF (EMPTYKO.T.RC = 0) THEN                                                
//WQSENKO EXEC WZ11P023,                                                        
//             DSIN=W152.W152V2.W152TOKO(+0)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.CBG.PARTSDESCRIPTION                                        
¤MQMPROP PhysicalId=P-%YYMMDD.ko                                                
¤MQMPROP Authorization=Basic Q0JHUFJPRDpGVFBQVUxTUFJPRA                         
/*                                                                              
//    ENDIF                                                                     
//*                                                                             
//****************************************************                          
//EMPTYNL EXEC WEMPTST,DSIN=W152.W152V2.W152TONL(+0)                            
//    IF (EMPTYNL.T.RC = 0) THEN                                                
//WQSENNL EXEC WZ11P023,                                                        
//             DSIN=W152.W152V2.W152TONL(+0)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.CBG.PARTSDESCRIPTION                                        
¤MQMPROP PhysicalId=P-%YYMMDD.nl                                                
¤MQMPROP Authorization=Basic Q0JHUFJPRDpGVFBQVUxTUFJPRA                         
/*                                                                              
//    ENDIF                                                                     
//*                                                                             
//****************************************************                          
//EMPTYPL EXEC WEMPTST,DSIN=W152.W152V2.W152TOPL(+0)                            
//    IF (EMPTYPL.T.RC = 0) THEN                                                
//WQSENPL EXEC WZ11P023,                                                        
//             DSIN=W152.W152V2.W152TOPL(+0)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.CBG.PARTSDESCRIPTION                                        
¤MQMPROP PhysicalId=P-%YYMMDD.pl                                                
¤MQMPROP Authorization=Basic Q0JHUFJPRDpGVFBQVUxTUFJPRA                         
/*                                                                              
//    ENDIF                                                                     
//*                                                                             
//****************************************************                          
//EMPTYPT EXEC WEMPTST,DSIN=W152.W152V2.W152TOPT(+0)                            
//    IF (EMPTYPT.T.RC = 0) THEN                                                
//WQSENPT EXEC WZ11P023,                                                        
//             DSIN=W152.W152V2.W152TOPT(+0)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.CBG.PARTSDESCRIPTION                                        
¤MQMPROP PhysicalId=P-%YYMMDD.pt                                                
¤MQMPROP Authorization=Basic Q0JHUFJPRDpGVFBQVUxTUFJPRA                         
/*                                                                              
//    ENDIF                                                                     
//*                                                                             
//****************************************************                          
//EMPTYRU EXEC WEMPTST,DSIN=W152.W152V2.W152TORU(+0)                            
//    IF (EMPTYRU.T.RC = 0) THEN                                                
//WQSENRU EXEC WZ11P023,                                                        
//             DSIN=W152.W152V2.W152TORU(+0)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.CBG.PARTSDESCRIPTION                                        
¤MQMPROP PhysicalId=P-%YYMMDD.ru                                                
¤MQMPROP Authorization=Basic Q0JHUFJPRDpGVFBQVUxTUFJPRA                         
/*                                                                              
//    ENDIF                                                                     
//*                                                                             
//****************************************************                          
//EMPTYSV EXEC WEMPTST,DSIN=W152.W152V2.W152TOSV(+0)                            
//    IF (EMPTYSV.T.RC = 0) THEN                                                
//WQSENSV EXEC WZ11P023,                                                        
//             DSIN=W152.W152V2.W152TOSV(+0)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.CBG.PARTSDESCRIPTION                                        
¤MQMPROP PhysicalId=P-%YYMMDD.sv                                                
¤MQMPROP Authorization=Basic Q0JHUFJPRDpGVFBQVUxTUFJPRA                         
/*                                                                              
//    ENDIF                                                                     
//*                                                                             
//****************************************************                          
//EMPTYTH EXEC WEMPTST,DSIN=W152.W152V2.W152TOTH(+0)                            
//    IF (EMPTYTH.T.RC = 0) THEN                                                
//WQSENTH EXEC WZ11P023,                                                        
//             DSIN=W152.W152V2.W152TOTH(+0)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.CBG.PARTSDESCRIPTION                                        
¤MQMPROP PhysicalId=P-%YYMMDD.th                                                
¤MQMPROP Authorization=Basic Q0JHUFJPRDpGVFBQVUxTUFJPRA                         
/*                                                                              
//    ENDIF                                                                     
//*                                                                             
//****************************************************                          
//EMPTYTR EXEC WEMPTST,DSIN=W152.W152V2.W152TOTR(+0)                            
//    IF (EMPTYTR.T.RC = 0) THEN                                                
//WQSENTR EXEC WZ11P023,                                                        
//             DSIN=W152.W152V2.W152TOTR(+0)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.CBG.PARTSDESCRIPTION                                        
¤MQMPROP PhysicalId=P-%YYMMDD.tr                                                
¤MQMPROP Authorization=Basic Q0JHUFJPRDpGVFBQVUxTUFJPRA                         
/*                                                                              
//    ENDIF                                                                     
//*                                                                             
//****************************************************                          
//EMPTYUS EXEC WEMPTST,DSIN=W152.W152V2.W152TOUS(+0)                            
//    IF (EMPTYUS.T.RC = 0) THEN                                                
//WQSENUS EXEC WZ11P023,                                                        
//             DSIN=W152.W152V2.W152TOUS(+0)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.CBG.PARTSDESCRIPTION                                        
¤MQMPROP PhysicalId=P-%YYMMDD.us                                                
¤MQMPROP Authorization=Basic Q0JHUFJPRDpGVFBQVUxTUFJPRA                         
/*                                                                              
//    ENDIF                                                                     
//*                                                                             
//****************************************************                          
//EMPTYZH EXEC WEMPTST,DSIN=W152.W152V2.W152TOZH(+0)                            
//    IF (EMPTYZH.T.RC = 0) THEN                                                
//WQSENZH EXEC WZ11P023,                                                        
//             DSIN=W152.W152V2.W152TOZH(+0)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.CBG.PARTSDESCRIPTION                                        
¤MQMPROP PhysicalId=P-%YYMMDD.zh                                                
¤MQMPROP Authorization=Basic Q0JHUFJPRDpGVFBQVUxTUFJPRA                         
/*                                                                              
//    ENDIF                                                                     
//*                                                                             
//****************************************************                          
//*                                                                             
//EJTOM IF (EMPTYCN.T.RC = 0) OR (EMPTYDE.T.RC = 0) OR                          
//         (EMPTYEN.T.RC = 0) OR (EMPTYES.T.RC = 0) OR                          
//         (EMPTYFI.T.RC = 0) OR (EMPTYFR.T.RC = 0) OR                          
//         (EMPTYIT.T.RC = 0) OR (EMPTYJA.T.RC = 0) OR                          
//         (EMPTYKO.T.RC = 0) OR (EMPTYNL.T.RC = 0) OR                          
//         (EMPTYPL.T.RC = 0) OR (EMPTYPT.T.RC = 0) OR                          
//         (EMPTYRU.T.RC = 0) OR (EMPTYSV.T.RC = 0) OR                          
//         (EMPTYTH.T.RC = 0) OR (EMPTYTR.T.RC = 0) OR                          
//         (EMPTYUS.T.RC = 0) OR (EMPTYZH.T.RC = 0) THEN                        
//*                                                                             
//MAIL1   EXEC WMAILSND                                                         
)SEND                                                                           
  TITLE P-&DAGID.                                                               
  DEST  puls@cbg.se                                                             
  MAIL TXTD1                                                                    
)END                                                                            
//TXTD1    DD DSN=W.QASE.CONSTANT(W15222ME),DISP=SHR                            
//         DD *                                                                 
  som börjar på... P-&DAGID..**                                                 
                                                                                
Debiteringsinfo:                                                                
  Översättning av ...                                                           
  PBEN poster i ovanstående filer                                               
                skall debiteras mot Volvo Orderkonto=&ORDBEN                    
  Övriga poster skall debiteras mot Volvo Orderkonto=&ORDLEX                    
//SYSABEND DD SYSOUT=*                                                          
//SYSOUT   DD SYSOUT=*                                                          
//*                                                                             
//TOMELSE  ELSE  -- TOM KOMMANDO-FIL                                            
//TOMMAIL EXEC WMAILSND                                                         
)SEND                                                                           
  TITLE P-&DAGID. inget data                                                    
  DEST MBEAUSAN@volvocars.com                                                   
  MAIL                                                                          
  Rutin W152V2 har inte skapat något data att skicka.                           
                                                                                
  MQ-sänding och Mail till CBG har därför hoppats över.                         
)END                                                                            
//TOMEND   ENDIF                                                                
//*                                                                             
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *               
//SOPEND  EXEC WSOPEND,PROCESS=W15222FT                                         
