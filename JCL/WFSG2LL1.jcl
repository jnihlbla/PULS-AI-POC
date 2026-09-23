//WFSG2LL1 JOB (650W3300100WFSG2LL1,W100),'RTN W330R1',                         
//             CLASS=K,TIME=(5,0),                                              
//             USER=?,PASSWORD=?                                                
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,CARDS=0                                                    
//*+JBS BIND D2G0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//LOAD     EXEC WG02LOAD,DSIN=W330.W330R1.W33021(+0),                           
//             TTLOAD=FSG2LO,UID=WFSG2LL1,JOBNAME=WFSG2LL1                      
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WFSG2LL1                                         
