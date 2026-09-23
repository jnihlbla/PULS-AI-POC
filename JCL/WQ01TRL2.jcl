//WQ01TRL2 JOB (640W4830100WQ01TRL2,W100),'RTN W483V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0,LINES=9999                                        
/*ROUTE   XEQ  LOCAL                                                            
/*ROUTE  PRINT LOCAL                                                            
//*+JBS BIND D2G0                                                               
//*                                                                             
//* - FÖLJANDE KAN ANVÄNDAS VID RECOVER EFTER ABENDER I RG02LOAD                
//* - LÄGG IN RÄTT GENERATION PÅ SENASTE VALIDA DUMPFIL.                        
//* - OCH TA BORT KOMMENTAR-* (TA BORT //* HELT)                                
//* - KÖR TVÅ GÅNGER - FÖRST TERMINATE + RECOVER SEPARAT OCH SEDAN              
//* - BARA WG02LOAD PÅ NORMALT SÄTT.                                            
//*                                                                             
//*//DB2CMD EXEC WG02CMD,CMD='-TERM UTIL(WQ01TRL2)'                             
//*//UTIL   EXEC DSNUPROC,SYSTEM=D2G0,UID='WQ01TRR2',UTPROC=''                  
//*//DSNUPROC.SYSIN    DD  *                                                    
//* RECOVER TABLESPACE DWQ01.SQ1TRPUP TOCOPY                                    
//*    WQ01.DUMP.SQ1TRPUP.G0XXXV00                                              
//*  REBUILD INDEX (ALL) TABLESPACE DWQ01.SQ1TRPUP                              
//*                                                                             
//DB2LOAD EXEC WG02LOAD,                                                        
//             UID=WQ01TRL2,JOBNAME=WQ01TRL2,                                   
//             DSIN=W483.W483V2.W48325(+0),                                     
//             TTLOAD=TQ1TRPL2                                                  
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WQ01TRL2                                         
