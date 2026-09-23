//WDBINDL1 JOB (640W0020200WDBINDL1,W100),'RTN W992V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.PROD.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVPROD                                                   
/*JOBPARM FORMS=1800,CARDS=0                                                    
/*ROUTE XEQ   NJEV2                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND D2Q0                                                               
//*                                                                             
//LOAD     EXEC WG02LOAD,DSIN=W992.W992V1.W99202(+0),INDRTE=W.IGRT,             
//             TTLOAD=DBINDLO,UID=WDBINDL1,JOBNAME=WDBINDL1                     
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=WDBINDL1                                         
