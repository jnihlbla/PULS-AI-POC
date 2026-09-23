//WDAGEND  JOB (540W0090100WDAGEND,W100),'RTN W981D1',                          
//             USER=?,PASSWORD=?,                                               
//        CLASS=L                                                               
//PROC  JCLLIB ORDER=(W.PROD.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVPROD                                                   
/*JOBPARM LINES=5,FORMS=1800,LINECT=0                                           
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
/*CNTL  WSOPDAT,EXC                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
END WDAGEND                                                                     
IF-STATUS WDAG STARTED                                                          
  END WDAG                                                                      
ENDIF                                                                           
//*                                                                             
// EXEC WWAIT,SECONDS=60   -- GE EV PÅGÅENDE RUTIN CHANSEN ATT GÅ KLAR          
