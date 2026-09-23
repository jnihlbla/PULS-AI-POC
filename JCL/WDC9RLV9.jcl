//WDC9RLV9 JOB (640W0020200WDC9LRV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDC9CLU                                          
//*                                                                             
//WDC9    EXEC WG01REL,                                                         
//             DBD=WDC9                                                         
//REL.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(NEW,PASS,DELETE),                        
//             SPACE=(4096,(5400,180),RLSE),                                    
//             DCB=BUFNO=10                                                     
//REL.DBORELD1 DD DSN=WG01.UNLO.WDC9K(+0),DISP=SHR                              
//REL.WDC9K DD DSN=WG01.QASE.WDC9K,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD DSN=WG01.QASE.WDC9K,DISP=SHR                                      
//SYSIN    DD DSN=W.QASE.CONSTANT(WDC9ACLU),DISP=SHR                            
//*                                                                             
//        EXEC WG01SIU                                                          
//SIU.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(OLD,PASS,DELETE)                         
//SIU.WDC9AK DD DSN=WG01.QASE.WDC9AK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:41                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDC9,INDD=DFSURWF1,ICNEEDED=OFF,                 
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDC9RLV9                                         
