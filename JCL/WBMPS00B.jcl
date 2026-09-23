//WBMPS00B JOB (540W0090100WBMPS00B,W100),'RTN W980D1',                         
//            USER=?,PASSWORD=?,                                                
//            CLASS=L                                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=5,FORMS=1800                                                    
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WBMPS00B                                         
