//WDAYS014 JOB (540W0090100WDAYS014,W100),'RTN W980D1',                         
//          USER=?,PASSWORD=?,                                                  
//        CLASS=L                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=5,FORMS=1800                                                    
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDAYS014                                         
