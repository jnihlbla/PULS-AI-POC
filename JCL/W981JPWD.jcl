//W981JPWD JOB (650W0090100W981JPWD,W100),'RTN W981R1',                         
//        USER=?,PASSWORD=?,                                                    
//        CLASS=L                                                               
//*                                                                             
//PROC  JCLLIB ORDER=(W.PROD.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVPROD                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ   NJESD                                                             
/*ROUTE PRINT NJOV2                                                             
//*+JBS BIND D2Q0                                                               
//*                                                                             
//SOPPWD EXEC W001ISPF                                                          
//SYSTSIN DD  *                                                                 
ISPSTART CMD(SOPPWD D20Q W.PROD )                                               
/*                                                                              
//VRCABE EXEC  VRCABEND,COND=(8,GT,SOPPWD.TSO)                                  
//*                                                                             
//SOPAB  EXEC WSOP,COND=ONLY                                                    
ABEND W981JPWD                                                                  
