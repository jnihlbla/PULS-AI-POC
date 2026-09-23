//WFSG5LL1 JOB (650W3300100WFSG5LL1,W100),'RTN W330R1',                         
//             CLASS=K,TIME=(20,0),                                             
//             USER=?,PASSWORD=?                                                
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,CARDS=0                                                    
//*+JBS BIND D2G0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//LOAD     EXEC WG02LOAD,DSIN=W330.W330R1.W33032(+0),                           
//             TTLOAD=FSG5LO,UID=WFSG5LL1,JOBNAME=WFSG5LL1                      
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WFSG5LL1                                         
