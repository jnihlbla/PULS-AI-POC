//WVUSERL1 JOB (640W0000100WVUSERL1,W100),'RTN W970B2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.PROD.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVPROD                                                   
/*JOBPARM FORMS=1800,CARDS=0                                                    
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND D2Q0                                                               
//*                                                                             
//LOAD     EXEC WG02LOAD,DSIN=W970.W970B2.W97001(+0),                           
//             INDRTE=W.IGRT,SYSTEM=D20Q,                                       
//             TTLOAD=VUSERLO,UID=WVUSERL1,JOBNAME=WVUSERL1                     
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=WVUSERL1                                         
