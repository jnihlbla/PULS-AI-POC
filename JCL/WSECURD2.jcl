//WSECURD2 JOB (640W0020200WSECURD2,W100),'RTN W012V2',                         
//             CLASS=K,                                                         
//             USER=?,PASSWORD=?                                                
//PROC  JCLLIB ORDER=(W.PROD.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVPROD                                                   
/*JOBPARM FORMS=1800,CARDS=0                                                    
/*ROUTE XEQ   NJESD                                                             
/*ROUTE PRINT NJOV2                                                             
//*+JBS BIND D2Q0                                                               
//*                                                                             
//DUMP    EXEC WG02DUMP,DSOUT=WG02T.DUMP.SECURE(+1),                            
//             DB2SYS=D20Q,                                                     
//             UID=WSECURD2                                                     
COPY TABLESPACE DWSECU.SECURE                                                   
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WSECURD2                                         
