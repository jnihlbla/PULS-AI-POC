//WQ01TRD1 JOB (640W4830100WQ01TRD1,W100),'RTN W483V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE    XEQ LOCAL                                                            
/*ROUTE  PRINT LOCAL                                                            
//*+JBS BIND D2G0                                                               
//*                                                                             
//DUMP     EXEC WG02DUMP,                                                       
//             UID=WQ01TRD1,                                                    
//             DSOUT=WQ01.DUMP.SQ1TRPPS(+1)                                     
//SYSCOPY DD SPACE=,DATACLAS=PSEB                                               
COPY     TABLESPACE DWQ01.SQ1TRPPS                                              
RUNSTATS TABLESPACE DWQ01.SQ1TRPPS   INDEX(ALL)                                 
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WQ01TRD1                                         
//*                                                                             
//* KAN ANVÄNDAS VID RECOVER EFTER ABENDER I LADDNINGEN:                        
//*                                                                             
//*UTIL EXEC DSNUPROC,SYSTEM=D2G0,UID='WQ01TRR1',UTPROC=''                      
//*DSNUPROC.SYSIN    DD  *                                                      
//* RECOVER TABLESPACE DWQ01.SQ1TRPPS TOCOPY                                    
//*    WQ01.DUMP.SQ1TRPPS.G0XXXV00                                              
//*  REBUILD INDEX (ALL) TABLESPACE DWQ01.SQ1TRPPS                              
