//W981JDAT JOB (650W0090100W981JDAT,W100),'RTN W981D1',                         
//        USER=?,PASSWORD=?,                                                    
//        CLASS=L                                                               
//PROC  JCLLIB ORDER=(W.PROD.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVPROD                                                   
/*JOBPARM LINES=50,FORMS=1800                                                   
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
/*CNTL  WSOPDAT,EXC                                                             
//*                                                                             
//DATCHG  EXEC WSOPDAT                                                          
//*                                                                             
//VRCABE EXEC  VRCABEND,COND=(8,GT)                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W981JDAT                                         
