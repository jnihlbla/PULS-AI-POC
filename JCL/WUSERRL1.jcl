//WUSERRL1 JOB (640W0000100WUSERRL1,W100),'RTN W970D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.PROD.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVPROD                                                   
/*JOBPARM FORMS=1800,CARDS=0                                                    
/*ROUTE XEQ   NJEV2                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND D2Q0                                                               
//*                                                                             
//LOAD     EXEC WG02LOAD,DSIN=W970.W970D1.W97010(+0),                           
//             INDRTE=W.IGRT,                                                   
//             TTLOAD=USERSLO,UID=WUSERRL1,JOBNAME=WUSERRL1                     
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=WUSERRL1                                         
