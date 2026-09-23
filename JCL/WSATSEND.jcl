//WSATSEND JOB (540W0090100WSATSEND,W100),'RTN W981D1',                         
//             USER=?,PASSWORD=?,                                               
//        CLASS=L                                                               
//PROC  JCLLIB ORDER=(W.PROD.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVPROD                                                   
/*JOBPARM LINES=5,FORMS=1800,LINECT=0                                           
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WSATSEND                                         
