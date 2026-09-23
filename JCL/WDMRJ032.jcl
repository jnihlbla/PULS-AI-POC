//WDMRJ032 JOB (640W0030200WDMRJ032,W100),'RTN WDMRV5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ NJEVC                                                             
/*ROUTE PRINT LOCAL                                                             
//LIST    EXEC  FDIRLIST,LIB='W.PROD.LOAD',                                     
//        SELECT=ALL,TITLE=NO                                                   
//V1645JD4 DD DSN=WDMR.WDMRV5.WDMR32(+1),                                       
//        DISP=(NEW,CATLG,DELETE),BLKSIZE=23440,                                
//        MGMTCLAS=BACKUPC,SPACE=(TRK,(1,1))                                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WDMRJ032                                         
