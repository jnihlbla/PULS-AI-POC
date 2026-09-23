//WFSG5DD1 JOB (650W3300100WFSG5DD1,W100),'RTN W330R1',                         
//             CLASS=K,TIME=(5,0),                                              
//             USER=?,PASSWORD=?                                                
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,CARDS=0                                                    
//*+JBS BIND D2G0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//DUMP    EXEC WG02DUMP,DSOUT=WG02.DUMP.FSG5(+1),                               
//             UID=WFSG5DD1                                                     
//DUMP.SYSCOPY DD SPACE=(CYL,(80,30),RLSE)                                      
COPY TABLESPACE DWFSG1.FSG5                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WFSG5DD1                                         
