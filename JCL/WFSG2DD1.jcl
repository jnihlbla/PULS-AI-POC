//WFSG2DD1 JOB (650W3300100WFSG2DD1,W100),'RTN W330R1',                         
//             CLASS=K,TIME=(5,0),                                              
//             USER=?,PASSWORD=?                                                
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,CARDS=0                                                    
//*+JBS BIND D2G0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//DUMP    EXEC WG02DUMP,DSOUT=WG02.DUMP.FSG2(+1),                               
//             UID=WFSG2DD1                                                     
COPY TABLESPACE DWFSG1.FSG2                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WFSG2DD1                                         
