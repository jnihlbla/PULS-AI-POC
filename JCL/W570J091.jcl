//W570J091 JOB (650W5700100W570J091,W100),'RTN W570Y1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W570    EXEC W570P091                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W570J091                                         
