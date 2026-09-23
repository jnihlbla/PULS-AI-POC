//W271JESC JOB (510WOS39000VILM2T),'FIX PÅ WDK711',                             
//             MSGCLASS=H,MSGLEVEL=(1,1),                                       
//             CLASS=E,NOTIFY=V063557                                           
/*JOBPARM ROOM=PVV1,LINES=25,CARDS=0,FORMS=STD,LINECT=00                        
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//PROC  JCLLIB ORDER=(W.FIXA.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//MSGOUT OUTPUT JESDS=ALL,DEFAULT=YES,                                          
//  ROOM=PVV1,DEPT='09232',ADDRESS=('VIT',                                      
//  '405 08','GOTHENBURG')                                                      
//*                                                                             
//*                                                                             
//WDK7     EXEC W271PESC,STPLIB=W.FIXA.LOAD,                                    
//             INDIN=V063557.FIXA                                               
//*                                                                             
//W271ESC.SYSUDUMP DD SYSOUT=*                                                  
//*                                                                             
//*END     EXEC WSOPEND,PROCESS=WDJ9JUPD,                                       
//             SOPREG=W.FIXA.SOP,                                               
//             PDSLIB=W.FIXA.JCL,PDSTEMP=W.FIXATMP.JCL                          
