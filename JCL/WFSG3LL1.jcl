//WFSG3LL1 JOB (650W3300100WFSG3LL1,W100),'RTN W330R1',                         
//             CLASS=K,TIME=(15,0),                                             
//             USER=?,PASSWORD=?                                                
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,CARDS=0                                                    
//*+JBS BIND D2G0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//LOAD     EXEC WG02LOAD,DSIN=W330.W330R1.W33023(+0),                           
//             TTLOAD=FSG3LO,UID=WFSG3LL1,JOBNAME=WFSG3LL1                      
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WFSG3LL1                                         
