//WQ01TRD2 JOB (640W4830100WQ01DRD2,W100),'RTN W483V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE    XEQ LOCAL                                                            
/*ROUTE  PRINT LOCAL                                                            
//*+JBS BIND D2G0                                                               
//*                                                                             
//DUMP     EXEC WG02DUMP,                                                       
//             UID=WQ01DRD2,                                                    
//             DSOUT=WQ01.DUMP.SQ1TRPUP(+1)                                     
//SYSCOPY DD SPACE=,DATACLAS=PSEB                                               
COPY     TABLESPACE DWQ01.SQ1TRPUP                                              
RUNSTATS TABLESPACE DWQ01.SQ1TRPUP   INDEX(ALL)                                 
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WQ01TRD2                                         
//*                                                                             
//* KAN ANVÄNDAS VID RECOVER EFTER ABENDER I LADDNINGEN:                        
//*                                                                             
//*UTIL EXEC DSNUPROC,SYSTEM=D2G0,UID='WQ01TRR2',UTPROC=''                      
//*DSNUPROC.SYSIN    DD  *                                                      
//* RECOVER TABLESPACE DWQ01.SQ1TRPUP TOCOPY                                    
//*    WQ01.DUMP.SQ1TRPUP.G0XXXV00                                              
//*  REBUILD INDEX (ALL) TABLESPACE DWQ01.SQ1TRPUP                              
