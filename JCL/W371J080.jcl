//W371J080 JOB (650W3710100W371J080,W100),'RTN W371V8',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
/*JOBPARM LINES=999                                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W371    EXEC W371P080                                                         
//*                                                                             
//********************************************************************          
//*                                                                  *          
//*    OM FIL W37173 INTE ÄR TOM SKICKAS ETT MEMO TILL MOTTAGARE     *          
//*    SOM DEFINIERATS I BILD 3124 (WDGX3156)                        *          
//*                                                                  *          
//********************************************************************          
/*AFTER MEMOAPIH                                                                
//EMPTYT1 EXEC WEMPTST,DSIN=W371.W371V8.W37173(+1)                              
//*                                                                             
//    IF (EMPTYT1.T.RC = 0) THEN                                                
//W371   EXEC WMEMOSND                                                          
//M.APIFILE DD DSN=W371.W371V8.W37174(+1),DISP=SHR                              
//          DD DSN=W371.W371V8.W37173(+1),DISP=SHR                              
//          DD DSN=W.QASE.CONSTANT(W371V8ME),DISP=SHR                           
//M.SEND    DD DUMMY                                                            
//    ENDIF                                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W371J080                                         
